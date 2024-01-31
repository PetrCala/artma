import React from 'react';
import Image from "next/image"
import CONST from '@/CONST';

const PlaceholderImage: React.FC = () => {
    return (
        <div>
            <Image
                src={CONST.THEME.PLACEHOLDER}
                width={36}
                height={36}
                sizes="36x36"
                alt="Placeholder"
                style={{ backgroundColor: "black" }}
            />
        </div>
    );
};

export default PlaceholderImage;
